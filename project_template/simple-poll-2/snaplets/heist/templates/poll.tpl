<apply template="base">

<bind tag="page-title">Snap example: simple poll</bind>

<bind tag="page-body">
<form method="post" action="/results">
<b>What is the best Haskell book for beginners?</b><br>
<form-content />
<input type="submit" name="submit" value="Submit"><br>
</form>
</bind>

</apply>
